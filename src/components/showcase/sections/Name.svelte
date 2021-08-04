<script>
  import github from "bootstrap-icons/icons/github.svg";

  export let sections;
</script>

<div class="h-screen w-screen flex flex-col">
  <div class="name text-right">
    <h2 class="inline text-lg font-mono border-b-2 pl-4 pb-2 mb-2">My Name</h2>
    <span
      class="block my-2 text-7xl sm:text-8xl md:text-9xl lg:text-10xl font-serif font-extrabold"
      >Max <br /> Niederman</span
    >
  </div>

  <div class="m-4 flex flex-row justify-between items-end">
    <a href="https://github.com/max-niederman" target="_blank" class="p-2">
      <img
        src={github}
        alt="GitHub logo"
        height="40"
        width="40"
        class="filter dark:invert"
      />
    </a>

    <a
      href={`#${sections[1].id}`}
      tabindex="0"
      class="hidden lg:block left-0 p-2 text-2xl font-mono font-bold"
    >
      Scroll =>
    </a>
  </div>
</div>

<style lang="scss">
  .name {
    flex-grow: 1;
    margin-top: calc(10vh + 100px);
    padding-right: 5vw;
  }
</style>
