<script lang="ts">
	import { onMount } from "svelte";
	import type { HTMLAttributeAnchorTarget } from "svelte/elements";

	export let hrefBase64: string;
	export let target: HTMLAttributeAnchorTarget = "_self";

	let href: string = "";

    onMount(() => {
        href = atob(hrefBase64);
    });
</script>

<a href={href} target={target}>
	<slot />
</a>
