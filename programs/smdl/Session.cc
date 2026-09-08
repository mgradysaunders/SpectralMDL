#include <fstream>
#include <iostream>

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Session.h"

void setUpCompiler(const Options &opts, smdl::Compiler &compiler) {
  compiler.isDebugEnabled = opts.compile.isDebugEnabled;
  compiler.shouldEmitUnitTests = true;
  compiler.colorMode = opts.utility.colorMode;
  compiler.wavelengthBaseMax = uint32_t(opts.compile.wavelengths.size());
  for (const auto &input : opts.inputs)
    if (auto error{compiler.add(input)}) error->printAndExit();
}

void writeOutput(const Options &opts, std::string_view text) {
  if (!opts.output.fileName.wasGiven) {
    std::cout << text;
    std::cout.flush();
    return;
  }
  const auto &fileName{opts.output.fileName.value};
  auto ofs{std::ofstream(fileName)};
  if (!ofs.is_open())
    throw smdl::Error(
        smdl::concat("cannot open ", smdl::Quoted(fileName), " for writing"));
  ofs << text;
}

void runDump(const Options &opts, smdl::Compiler &compiler) {
  if (auto error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  auto dumped{std::string{}};
  if (auto error{compiler.dump(opts.output.dumpFormat, dumped)})
    error->printAndExit();
  writeOutput(opts, dumped);
}

void runList(const Options &opts, smdl::Compiler &compiler) {
  if (auto error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  std::cout << compiler.printMaterialSummary();
  std::cout.flush();
}

void runFormat(const Options &opts, smdl::Compiler &compiler) {
  if (auto error{compiler.formatSourceFiles(opts.format)})
    error->printAndExit();
}
