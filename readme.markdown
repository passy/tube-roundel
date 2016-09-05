# tube-roundel
[![Build Status](https://travis-ci.org/passy/tube-roundel.svg?branch=master)](https://travis-ci.org/passy/tube-roundel)
[![](https://images.microbadger.com/badges/version/passy/tube-roundel.svg)](https://hub.docker.com/r/passy/tube-roundel/)
[![](https://images.microbadger.com/badges/image/passy/tube-roundel.svg)](https://microbadger.com/#/images/passy/tube-roundel)

> A simple Haskell web-service for rendering TfL Tube Roundels in different
> colors as either PNG or SVG.

<img align="left" width="100" src="https://tube-roundel.rdrei.net/roundel/no-text/F8B195/image.svg">
<img align="left" width="100" src="https://tube-roundel.rdrei.net/roundel/no-text/F67280/image.svg">
<img align="left" width="100" src="https://tube-roundel.rdrei.net/roundel/no-text/C06C84/image.svg">
<img align="left" width="100" src="https://tube-roundel.rdrei.net/roundel/no-text/6C5B7B/image.svg">
<img width="100" src="https://tube-roundel.rdrei.net/roundel/no-text/355C7D/image.svg">

## Endpoints

Currently, only one endpoint is supported which lets you generate a single
roundel without any text in color you specify as hex color code.

- `/roundel/no-text/:color/image.svg` as `image/svg+xml`
  - e.g. `/roundel/no-text/F8B195/image.svg`
- `/roundel/no-text/:color/image.png` as `image/png`
  - e.g. `/roundel/no-text/F8B195/image.png`

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

## License

Licensed under 3-clause BSD.
