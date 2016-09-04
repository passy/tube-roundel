# tube-roundel
[![Build Status](https://travis-ci.org/passy/tube-roundel.svg?branch=master)](https://travis-ci.org/passy/tube-roundel)
[![](https://images.microbadger.com/badges/image/passy/tube-roundel.svg)](https://microbadger.com/images/passy/tube-roundel "Get your own image badge on microbadger.com")


> A simple Haskell web-service for rendering TfL Tube Roundels in different
> colors as either PNG or SVG.

## Setup

```
stack setup
stack build
env PORT=8080 HOSTNAME=localhost stack run tube-roundel &
curl -vv http://127.0.0.1:8080/roundel/no-text/445566/image.svg
```

## Deployment

You can run this directly as a Docker container which is built on
[Docker Hub](https://hub.docker.com/r/passy/tube-roundel/).

```
docker run --rm -p 127.0.0.1:8080:8080 passy/tube-roundel:$VERSION
```

Where `$VERSION` is the last published
[release](https://github.com/passy/tube-roundel/releases). To simplify the
build process, I directly publish the Travis artifacts and don't rebuild
inside the Docker container which is why I limit this to tagged releases.

One nice additional benefit of using a container is that building the font
cache is *much* faster than on a desktop machine as there are
*no fonts to cache*. Clearly, this is something that should be addressed in
code, but for now, it works.
