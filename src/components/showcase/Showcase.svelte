<script>
  import { onMount } from "svelte";
  import { fly } from "svelte/transition";
  import Nav from "./Nav.svelte";
  import sections from "./sections";

  let scrollContainer;
  let current = sections[0];

  function updatePosition() {
    if (scrollContainer.scrollWidth > scrollContainer.clientWidth) {
      sections.sort((a, b) => a.element.offsetLeft - b.element.offsetLeft);

      current = sections.find(
        ({ element }) => element.offsetLeft + 500 >= scrollContainer.scrollLeft
      );
    } else {
      sections.sort((a, b) => a.element.offsetTop - b.element.offsetTop);

      current = sections.find(
        ({ element }) => element.offsetTop >= scrollContainer.scrollTop
      );
    }
  }

  onMount(() => {
    scrollContainer.addEventListener("wheel", (e) => {
      if (scrollContainer.scrollWidth > scrollContainer.clientWidth) {
        e.preventDefault();
        scrollContainer.scrollLeft += e.deltaY * 2;
      }
    });

    scrollContainer.addEventListener("scroll", updatePosition);
    updatePosition();
  });
</script>

<Nav
  sections={sections
    .filter(({ shortTitle }) => shortTitle !== undefined)
    .map(({ id, shortTitle }) => ({ title: shortTitle, link: `#${id}` }))}
  home={`#${sections[0].id}`}
  open={current === sections[0]}
/>

<div class="hidden lg:block fixed z-10 right-24 top-12">
  {#each sections as section}
    {#if current === section}
      <h1 class="text-6xl font-serif font-extrabold" in:fly={{ y: -25, duration: 200 }} out:fly={{ duration: 0 }}>{section?.title ?? ""}</h1>
    {/if}
  {/each}
</div>

<div bind:this={scrollContainer} class="scroll-container">
  {#each sections as section}
    <div bind:this={section.element} id={section.id}>
      <svelte:component this={section.content} sections={sections} />
    </div>
  {/each}
</div>

<style lang="scss">
  .scrollContainer {
    scroll-behavior: smooth;
  }

  @media (min-width: 1024px) {
    .scroll-container {
      height: 100vh;
      width: 100vw;

      overflow-x: scroll;
      overflow-y: hidden;

      scrollbar-width: none;


      display: flex;
      flex-direction: row;
      flex-wrap: nowrap;

      * {
        scroll-snap-align: start;
        display: inline-block;
        margin: 0;
      }
    }
  }
</style>
