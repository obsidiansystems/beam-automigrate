let pkgs = (import ./.ci/nixpkgs.nix).unstable;
    # dburl = db: pkgs.writeText "${db}-dburl" "postgres:${db}";
    # readmeDb = "readme";
    testUser = "test";
in pkgs.nixosTest ({
  skipLint = true;
  enableOCR = true;
  nodes.machine = { config, options, pkgs, lib, ... }: {
    services.postgresql = {
      enable = true;
      authentication = ''
        host all all 0.0.0.0/0 trust
      '';
      ensureDatabases = [
        "beam-test-db" # beam-automigrate-examples
        "groundhog-test-db" # beam-automigrate-examples
        "readme"
        "public" # beam-automigrate-integration-tests
        "beam-migrate-prototype-bench" # beam-automigrate-large-migration-test
      ];
    };
    services.xserver.enable = false;
    environment.systemPackages = with pkgs; [
      postgresql
      (import ./release.nix).compilers.ghc8107
    ];
    users.users.${testUser} = {
      createHome = true;
      isNormalUser = true;
    };
  };
  testScript = ''
    machine.start()
    machine.execute('logger -t TEST "Waiting for PostgreSQL to start..."')
    machine.wait_for_unit("postgresql.service")
    machine.wait_for_open_port(5432)
    machine.execute('logger -t TEST "PostgreSQL has started."')
    
    machine.execute('logger -t TEST "Running beam-automigrate-examples..."')
    machine.succeed("beam-automigrate-examples")

    machine.execute('logger -t TEST "Running readme..."')
    machine.succeed('readme ci')

    machine.execute('logger -t TEST "Running beam-automigrate-integration-tests..."')
    machine.succeed('beam-automigrate-integration-tests')

    # machine.execute('logger -t TEST "Running beam-automigrate-large-migration-test..."')
    # machine.succeed("beam-automigrate-large-migration-test")
  '';
})

