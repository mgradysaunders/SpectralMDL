# SpectralMDL for Visual Studio Code

Syntax highlighting and editing support for MDL and the SpectralMDL dialect,
the counterpart of `etc/vim/mdl.vim`. The extension is declarative: a TextMate
grammar (`syntaxes/mdl.tmLanguage.json`) and a language configuration, with
nothing to build.

## Install

From the repository root:

    ln -s "$PWD/etc/vscode" ~/.vscode/extensions/spectralmdl

then restart VSCode. Copying the directory there works too, but does not
follow later changes.

## Develop

Open `etc/vscode` in VSCode and press F5. This starts an Extension Development
Host on the repository with the grammar loaded. In it, "Developer: Inspect
Editor Tokens and Scopes" shows the scopes under the cursor, and "Developer:
Reload Window" picks up an edited grammar.

## Notes

- Scopes ending in `.smdl` mark the SpectralMDL extensions, as the `smdl*`
  groups do in the Vim file.
- Any `#name` is highlighted as an intrinsic, so adding one to the compiler
  needs no change here; the compiler diagnoses a misspelled one.
- Annotation blocks `[[ ... ]]` are not dimmed, because a comment scope would
  turn off bracket matching and quote auto-closing inside them. To dim them as
  the Vim file does, add this to `settings.json`:

      "editor.tokenColorCustomizations": {
        "textMateRules": [
          { "scope": "meta.annotation.mdl", "settings": { "foreground": "#808080" } }
        ]
      }

- Indentation is regex-based, so `case` labels and `namespace` bodies are
  indented one level where the Vim file keeps them flush.
