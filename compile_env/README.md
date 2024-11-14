## Compile Steps:
- ### 1. git clone https://github.com/IntersectMBO/plutus-apps.git 
- ### 2. cd plutus-app
- ### 3. git checkout v1.0.0-alpha1
- ### 4. nix-shell --extra-experimental-features flakes
- ### 5. cd {Path of the crosschain contract}
- ### 6. nix --extra-experimental-features "nix-command flakes" run .#cross-chain:exe:cross-chain --print-build-logs