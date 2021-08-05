<script>
  export let categories = [
    {
      title: "Latest",
      writings: [{ title: "Changelog", link: "/changelog" }],
    },
    {
      title: "Notable",
      writings: [{ title: "Changelog", link: "/changelog" }],
    },
  ];
</script>

<div
  class="min-w-screen min-h-screen p-2 sm:p-4 md:p-12 lg:p-48 flex flex-col lg:grid grid-rows-1 auto-cols-auto grid-flow-col"
>
  {#each categories as { title, writings }}
    <div class="flex-grow my-4 px-4 text-xl font-serif">
      <h2 class="text-5xl font-bold pb-4 mb-4 border-b-2">{title}</h2>

      <ul class="m-4 list-disc">
        {#each writings as { title, link }}
          <li><a class="underline" href={link}>{title}</a></li>
        {/each}
      </ul>

      <a href="/writings" class="font-mono font-bold"> More => </a>
    </div>
  {/each}
</div>
