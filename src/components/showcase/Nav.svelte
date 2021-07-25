<script>
  import { fly } from "svelte/transition";

  export let sections;
  export let home = "/";
  export let open = true;

  let hovered = false;
</script>

<nav
  class="fixed z-30 p-4 flex flex-row md:flex-col bg-white rounded-br-md"
  on:mouseenter={() => (hovered = true)}
  on:mouseleave={() => (hovered = false)}
>
  <a href={home} class="text-4xl leading-6 font-serif font-extrabold">M</a>

  {#if open || hovered}
    <div class="hidden md:flex flex-col items-center" transition:fly="{{ y: -15, duration: 50 }}">
      {#each sections as { title, link }}
        {#if title !== null}
          <a
            href={link}
            class="my-4 text-xl font-mono font-bold writing-mode-ve-lr transform rotate-180"
          >
            {title}
          </a>
        {/if}
      {/each}
    </div>
  {/if}

  <div class="flex md:hidden flex-row ml-6">
    {#each sections as { title, link }}
      {#if title !== null}
        <a
          href={link}
          class="text-right text-xl font-mono font-bold"
        >
          {title}
        </a>
      {/if}
    {/each}
  </div>
</nav>
