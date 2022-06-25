let pkgs = (import ./.ci/nixpkgs.nix).nixos2003;
in pkgs.nixosTest ({
  skipLint = true;
  machine = { config, options, pkgs, lib, ... }: {
    services.postgresql = {
      enable = true;
      authentication = ''
        host all all 0.0.0.0/0 trust
      '';
      ensureDatabases = [
        "beam-test-db"
        "groundhog-test-db"

        "beam-migrate-prototype-bench"
      ];
    };
    services.xserver.enable = false;
    environment.systemPackages = with pkgs; [
      postgresql
      (import ./release.nix).compilers.ghc8107
    ];
  };
  testScript = ''
    machine.start()
    machine.execute('logger -t TEST "Waiting for PostgreSQL to start..."')
    machine.wait_for_unit("postgresql.service")
    machine.wait_for_open_port("5432")
    machine.execute('logger -t TEST "PostgreSQL has started."')
    
    machine.execute('logger -t TEST "Running beam-automigrate-examples"')
    machine.succeed("beam-automigrate-examples")
  '';
})

