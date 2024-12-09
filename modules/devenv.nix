{inputs, ...}: {
  imports = [
    inputs.devenv.flakeModule
  ];

  perSystem = {
    pkgs,
    system,
    ...
  }: {
    devenv = {
      modules = [
        inputs.env-help.devenvModule
      ];
      shells.default = {
        enterShell = ''
          printf "Advent of Code 2024\n" | ${pkgs.lolcat}/bin/lolcat
          printf "\033[0;1;36mDEVSHELL ACTIVATED\033[0m\n"
        '';

        env-help.enable = true;

        languages = {
          haskell = {
            enable = true;
            package = pkgs.ghc.withPackages (_:
              with pkgs.haskellPackages; [
                hspec
                QuickCheck
                regex-tdfa
                stringsearch
                universe
              ]);
          };
          nix.enable = true;
        };

        pre-commit = {
          default_stages = ["pre-push"];
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            check-added-large-files = {
              enable = true;
              stages = ["pre-commit"];
            };
            check-yaml.enable = true;
            deadnix.enable = true;
            detect-private-keys = {
              enable = true;
              stages = ["pre-commit"];
            };
            end-of-file-fixer.enable = true;
            flake-checker.enable = true;
            gofmt.enable = true;
            golangci-lint.enable = true;
            govet.enable = true;
            hlint = {
              enable = true;
              entry = "${pkgs.haskellPackages.hlint}/bin/hlint";
              name = "hlint";
            };
            markdownlint.enable = true;
            mixed-line-endings.enable = true;
            nil.enable = true;
            no-commit-to-branch = {
              enable = true;
              stages = ["pre-commit"];
            };
            ripsecrets = {
              enable = true;
              stages = ["pre-commit"];
            };
            shellcheck.enable = true;
            shfmt.enable = true;
            snekcheck = {
              enable = true;
              entry = "${inputs.snekcheck.packages.${system}.snekcheck}/bin/snekcheck";
              name = "snekcheck";
            };
            statix.enable = true;
          };
        };

        scripts = {
          lint = {
            description = "Lints the project.";
            exec = ''
              ${pkgs.gum}/bin/gum spin --show-error --spinner line --title "nix fmt" -- \
                nix fmt
              ${pkgs.gum}/bin/gum spin --show-error --spinner line --title "hlint" -- \
                ${pkgs.haskellPackages.hlint}/bin/hlint .
            '';
          };
          run = {
            description = "Runs the puzzles for a specific day.";
            exec = ''
              if [[ "$#" -ne 1 ]]; then
                echo "Usage: $(basename "$0") <day>"
                exit 1
              fi
              day="$1"
              runghc "$DEVENV_ROOT"/src/"$day" run "$DEVENV_ROOT"/puzzles/"$day"_in "$DEVENV_ROOT"/puzzles/"$day"_out
            '';
          };
          unit = {
            description = "Runs unit tests for a specific day.";
            exec = ''
              if [[ "$#" -ne 1 ]]; then
                echo "Usage: $(basename "$0") <day>"
                exit 1
              fi
              day="$1"
              runghc "$DEVENV_ROOT"/src/"$day" test
            '';
          };
        };
      };
    };
  };
}
