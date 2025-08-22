#include "smdl/Support/Parallel.h"

#include "llvm/Support/Parallel.h"

#include <atomic>
#include <condition_variable>
#include <mutex>

namespace smdl {

// TODO Uh oh, this thread locks on my laptop
#if 0
void invoke_once_on_each_worker_thread(const std::function<void()> &func) {
  SMDL_SANITY_CHECK(func);
  auto taskGroup{llvm::parallel::TaskGroup()};
  auto taskMutex{std::mutex()};
  auto taskCondition{std::condition_variable()};
  auto threadCount{llvm::parallel::getThreadCount()};
  auto threadIndex{size_t(-1)};
  for (size_t i{}; i < threadCount; i++) {
    taskGroup.spawn([&]() {
      auto lock{std::unique_lock(taskMutex)};
      taskCondition.wait(lock, [&] {
        return llvm::parallel::getThreadIndex() == threadIndex;
      });
      ++threadIndex;
      taskCondition.notify_all();
      func();
    });
  }
  {
    auto lock{std::unique_lock(taskMutex)};
    threadIndex = 0;
  }
  taskCondition.notify_all();
  taskGroup.sync();
}

static thread_local bool isMainThread{};

static thread_local bool isWorkerThread{};

static const int dummyInitialize{[]() -> int {
  llvm::set_thread_name("main");
  isMainThread = true;
  invoke_once_on_each_worker_thread([]() {
    isWorkerThread = true;
    auto name{std::string("worker") +
              std::to_string(llvm::parallel::getThreadIndex())};
    llvm::set_thread_name(name);
  });
  return 0;
}()};

bool is_main_thread() noexcept { return isMainThread; }

bool is_worker_thread() noexcept { return isWorkerThread; }
#endif

int get_worker_thread_index() noexcept {
  return llvm::parallel::getThreadIndex();
}

int get_worker_thread_count() noexcept {
  return llvm::parallel::getThreadCount();
}

void parallel_for(size_t indexBegin, size_t indexEnd,
                  const std::function<void(size_t)> &func) {
  SMDL_SANITY_CHECK(func != nullptr);
  llvm::parallelFor(indexBegin, indexEnd, func);
}

} // namespace smdl
