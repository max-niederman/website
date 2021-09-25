#! /bin/sh

npm i -g pnpm
pnpm install

cd content
pnpm run build

mv ./dist ../