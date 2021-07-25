export default {
  alias: {
    "@app": "./src",
  },
  mount: {
    "node_modules/@fontsource": {
      url: "/fonts",
      static: true,
    },
  },
};
