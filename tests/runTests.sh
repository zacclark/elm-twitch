#!/bin/sh

set -e


elm-package install -y
elm-make --yes --output test.js Tests.elm
node test.js
