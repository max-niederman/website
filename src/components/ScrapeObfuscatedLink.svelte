<script lang="ts">
	import { onMount } from "svelte";
	import type { HTMLAttributeAnchorTarget } from "svelte/elements";

	export let hrefBase64: string;
	export let target: HTMLAttributeAnchorTarget = "_self";
	export let linkType: string | undefined = undefined;

	let href: string = "";

    onMount(() => {
        href = atob(hrefBase64);
    });
</script>

<a class="type" {href} {target} data-link-type={linkType}>
	<slot />
</a>

<style lang="scss">
	@use "~/styles/links.scss";
</style>