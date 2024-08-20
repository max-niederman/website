<script lang="ts">
	import { onMount } from "svelte";
	import { Gradient } from "whatamesh";

	let canvas: HTMLCanvasElement;

	onMount(() => {
		const gradient = new Gradient();
		gradient.initGradient(`canvas.${canvas.className}`);
	});
</script>

<canvas bind:this={canvas} />

<style lang="scss">
	canvas {
		width: 100%;
		height: 100%;

		--gradient-color-1: #449ce4;
		--gradient-color-2: #2f8bc1;
		--gradient-color-3: #ccbeee;
		--gradient-color-4: #4c57f6;
	}
</style>
