#!/usr/bin/env bash
activator stage
./target/universal/stage/bin/mypictures-api -Dlogback.configurationFile=/Users/adelegue/idea/mypictures-api/logback.xml -Dconfig.file=/Users/adelegue/idea/mypictures-api/application.prod.conf