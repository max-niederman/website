<script>
  import { onMount } from "svelte";
  import Nav from "./Nav.svelte";

  let container;
  let sections = [
    {
      title: null,
      shortTitle: null,
      link: "#name",
      element: undefined,
    },
    {
      title: "My Skills",
      shortTitle: "Skills",
      link: "#skills",
      element: undefined,
    },
    {
      title: "My Work",
      shortTitle: "Work",
      link: "#work",
      element: undefined,
    },
    {
      title: "My Writings",
      shortTitle: "Writings",
      link: "#writings",
      element: undefined,
    },
  ];
  let current;

  onMount(() => {
    container.addEventListener("wheel", (e) => {
      e.preventDefault();
      container.scrollLeft += e.deltaY;
    });

    container.addEventListener("scroll", ({ target }) => {
      sections.sort((a, b) => a.element.offsetLeft - b.element.offsetLeft);
      console.log(sections.map(({ element }) => element.offsetLeft));
      current = sections.find(({ element }) => element.offsetLeft + 500 >= target.scrollLeft);
    });
  });
</script>

<Nav
  sections={sections
    .filter(({ shortTitle }) => shortTitle !== null)
    .map(({ shortTitle, link }) => ({ title: shortTitle, link }))}
/>

<div class="fixed z-10 right-24 top-12">
  <h1 class="text-6xl font-serif font-extrabold">{current?.title ?? ""}</h1>
</div>

<div bind:this={container} class="showcase-container">
  <div
    bind:this={sections[0].element}
    class="section h-screen w-screen"
    id="name"
  />
  <div
    bind:this={sections[1].element}
    class="section h-screen w-screen"
    id="skills"
  />
  <div
    bind:this={sections[2].element}
    class="section h-screen w-screen"
    id="work"
  />
  <div
    bind:this={sections[3].element}
    class="section h-screen w-screen"
    id="writings"
  />
</div>

<style lang="scss">
  .showcase-container {
    position: fixed;
    height: 100vh;
    width: 100vw;

    overflow-x: scroll;
    overflow-y: hidden;
    white-space: nowrap;

    scrollbar-width: none;

    .section {
      display: inline-block;
      margin: 0;
      margin-right: -4px;

      &:nth-of-type(1) {
        background: #ffa0a0;
      }

      &:nth-of-type(2) {
        background: #a0ffa0;
      }

      &:nth-of-type(3) {
        background: #a0a0ff;
      }

      &:nth-of-type(4) {
        background: #a0f0ff;
      }
    }
  }
</style>
