pipeline {
  agent any

  stages {
    stage('Nix Build') {
      steps {
        sh '''. ~/.nix-profile/etc/profile.d/nix.sh
              nix-shell --run exit | tee ./nix-build.log'''
      }
    }

    stage('Tests') {
      steps {
        sh '''. ~/.nix-profile/etc/profile.d/nix.sh
        # can't run test because it needs aws creds
        nix-shell --command "make all"'''
        }
      }
    }
  }
