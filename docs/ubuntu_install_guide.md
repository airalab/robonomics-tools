Install on Ubuntu 16.04
=======================

> I write this docs using Ubuntu 16.04 docker image.

    root@3ed3a4fca2f7:/# apt install curl bzip2 sudo

Install Stack
-------------

    root@3ed3a4fca2f7:/# curl https://get.haskellstack.org/ | sh
    root@3ed3a4fca2f7:/# whereis stack
    stack: /usr/local/bin/stack

Install Nix
-----------

    user@3ed3a4fca2f7:/$ curl https://nixos.org/nix/install | sh
    user@3fb13c457a4b:/$ . /home/user/.nix-profile/etc/profile.d/nix.sh
    user@3fb13c457a4b:/$ whereis nix
    nix: /nix/store/cdcia67siabmj6li7vyffgv2cry86fq8-nix-2.1.3/bin/nix

Build
-----

    user@3fb13c457a4b:~$ git clone https://github.com/airalab/robonomics-tools
    Cloning into 'robonomics-tools'...
    remote: Enumerating objects: 42, done.
    remote: Counting objects: 100% (42/42), done.
    remote: Compressing objects: 100% (28/28), done.
    remote: Total 42 (delta 7), reused 42 (delta 7), pack-reused 0
    Unpacking objects: 100% (42/42), done.
    Checking connectivity... done. 
    user@3fb13c457a4b:~$ cd robonomics-tools/
    user@3fb13c457a4b:~/robonomics-tools$ stack build --nix

Run IPFS daemon
---------------

    user@3fb13c457a4b:~/robonomics-tools$ stack exec --nix ipfs init
    initializing IPFS node at /home/user/.ipfs
    generating 2048-bit RSA keypair...done
    peer identity: QmbNpZfcTA1HX7NnuzJsxLUC4V4CB1fZcaP3GaGAH4oEsV
    to get started, enter:

        ipfs cat /ipfs/QmS4ustL54uo8FzR9455qaxZwuMiUhyvMcX9Ba8nUH4uVv/readme

    user@3fb13c457a4b:~/robonomics-tools$ stack exec --nix ipfs -- daemon --enable-pubsub-experiment
    Initializing daemon...
    Successfully raised file descriptor limit to 2048.
    Swarm listening on /ip4/127.0.0.1/tcp/4001
    Swarm listening on /ip4/172.17.0.3/tcp/4001
    Swarm listening on /p2p-circuit/ipfs/QmbNpZfcTA1HX7NnuzJsxLUC4V4CB1fZcaP3GaGAH4oEsV
    Swarm announcing /ip4/127.0.0.1/tcp/4001
    Swarm announcing /ip4/172.17.0.3/tcp/4001
    API server listening on /ip4/127.0.0.1/tcp/5001
    Gateway (readonly) server listening on /ip4/127.0.0.1/tcp/8080
    Daemon is ready

Run XRTd daemon
---------------

    user@3fb13c457a4b:~/robonomics-tools$ stack exec --nix xrtd
    Missing: --private KEY

    Usage: xrtd [--web3 URI] --private KEY [--chain CHAIN_ID] [--ipfs MULTIADDR]
                [--lighthouse ENS] [--ens ADDRESS]
    XRTd :: Robonomics network provider

