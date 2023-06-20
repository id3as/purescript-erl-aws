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
        nix-shell --command "make all test"'''
        }
      }
    }
  }
