export default {
  alias: {
    $: "./src",
  },
  mount: {
    "node_modules/@fontsource": {
      url: "/fonts",
      static: true,
    },
  },
};
