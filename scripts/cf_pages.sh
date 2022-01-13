#! /bin/sh

npm i -g pnpm
pnpm install

pushd content
pnpm run build
popd

mv ./content/dist ./