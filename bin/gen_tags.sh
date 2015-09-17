#!/bin/bash

find . | egrep '\.hs$' | xargs hothasktags > tags
