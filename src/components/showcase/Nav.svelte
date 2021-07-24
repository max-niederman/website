<script>
  export let sections;
</script>

<nav class="fixed z-20 h-100 p-4 border-r-2">
  <a href="/" class="text-3xl leading-6 font-serif font-extrabold">M</a>
  <div class="flex flex-col">
    {#each sections as { title, link }}
      {#if title !== null}
        <a
          href={link}
          class="my-4 font-mono font-bold writing-mode-ve-lr transform rotate-180"
          >{title}</a
        >
      {/if}
    {/each}
  </div>
</nav>
