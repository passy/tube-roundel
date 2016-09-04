# tube-roundel

> A simple Haskell web-service for rendering TfL Tube Roundels in different
> colors as either PNG or SVG.

## Setup

```
stack setup
stack build
env PORT=8080 HOSTNAME=localhost stack run tube-roundel &
curl -vv http://127.0.0.1:8080/roundel/no-text/445566/image.svg
```
