# entranceBE - Written in Haskell

This is to (among other things) a learning experiment to test using MongoDB from Haskell, including accessing an already existing database (ie match the schema of the existing DB)

# Building and development

You need [Stack](https://docs.haskellstack.org/en/stable/README/) installed on your machine

To build a static executable, just run `stack build` in the root of the project

To run the tests, use `stack test`

# Deployment

The app can be deployed with `docker-compose`.

1. Run `docker-compose build`
2. Run `docker-compose up -d`

# Possible issues

### The build takes forever

This is only the first build because stack will download the correct version of the Haskell compiler and it builds all the dependencies. But both will be cached globally, so if you rebuild, or even build a different project, it won't take long

### The docker build fails with `Killed` on Mac

In the preferences of docker for mac, increase the memory cap e.g. to 8GB
