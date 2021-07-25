<script>
  export let works = [
    {
      title: "ttyper",
      summary:
        "Ttyper is a terminal-based typing test. No more bloated, web-based typing practice; just you, your keyboard, and your terminal emulator. I wrote ttyper using Rust and the excellent tui-rs library.",
      links: {
        internal: "/projects/ttyper",
        external: "https://github.com/max-niederman/ttyper",
      },
    },
    {
      title: "Luthien",
      summary:
        "Luthien is a CLI tool which generates and applies color schemes. It uses a custom method to extract colorschemes with usable contrast and consistent properties from images, and includes a highly pluggable system for applying colorschemes to a target system.",
      links: {
        internal: "/projects/luthien",
        external: "https://github.com/max-niederman/luthien",
      },
    },
    {
      title: "Lorem Ipsum",
      summary:
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
      links: {
        internal: "/projects/loremipsum",
        external: "https://github.com/max-niederman/loremipsum",
      },
    },
  ];
</script>

<div class="min-w-screen min-h-screen p-2 sm:p-4 md:p-12 lg:p-48 flex flex-col lg:grid grid-rows-1 auto-cols-auto grid-flow-col">
  {#each works as { title, summary, links }}
    <div class="flex-grow my-4 px-4 font-mono">
      <h2 class="text-5xl font-serif font-bold pb-4 mb-4 border-b-2">
        {title}
      </h2>

      <p class="my-2">{summary}</p>

      <a href={links.internal} class="font-bold italic">Read More =></a>
      <br class="my-1" />
      <a href={links.external} target="_blank" class="font-bold italic"
        >Open =>></a
      >
    </div>
  {/each}
</div>
