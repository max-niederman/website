<script lang="ts">
	import { onMount } from "svelte";

	/** Duration in seconds to move one content-width. */
	export let duration: number;
	/** Repetitions of the content assumed to be enough to cover one screen width. */
	export let count: number = 1;
	/** Gap in pixels between repetitions of the content. */
	export let gap: number = 0;

	let container: HTMLDivElement;

	onMount(() => {
		let firstContent = container.children[0] as HTMLDivElement;
		if (firstContent.scrollWidth * count < container.clientWidth) {
			count = Math.ceil(container.clientWidth / firstContent.scrollWidth);
		}
	});
</script>

<div class="container" style={`--gap: ${gap}px; --duration: ${duration}s;`} bind:this={container}>
	{#each [...Array(2 * count).keys()] as i (i)}
		<div class="content" aria-hidden={i != 0}>
			<slot />
		</div>
	{/each}
</div>

<style lang="scss">
	.container {
		width: 100%;
		height: 100%;

		display: flex;
		align-items: center;
		overflow: hidden;
		gap: var(--gap);

		&.loading {
			visibility: hidden;
		}
	}

	.content {
		display: flex;
		align-items: center;

		animation: marquee var(--duration) linear infinite;
	}

	@keyframes marquee {
		from {
			transform: translateX(calc(-100% - var(--gap)));
		}
		to {
			transform: translateX(0%);
		}
	}
</style>
