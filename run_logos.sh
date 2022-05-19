#! /bin/bash
if ! command -v java &> /dev/null
   then
   echo "java either is not installed or could not be found"
   exit
fi
if ! command -v clj &> /dev/null
then
    echo "clj either is not installed or could not be found"
    exit
fi
if ! command -v lein &> /dev/null
then
    echo "lein either is not installed or could not be found"
    exit
fi

if ! command -v npm %> /dev/null
then
    echo "npm either is not install or could not be found"
    exit
fi

lein run & npm run watch
