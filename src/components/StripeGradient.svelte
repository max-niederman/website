<script lang="ts">
	import { onMount } from "svelte";
	import { Gradient } from "whatamesh";

	let canvas: HTMLCanvasElement;

	onMount(() => {
		const gradient = new Gradient();
		gradient.initGradient(canvas);
	});
</script>

<canvas bind:this={canvas} />

<style lang="scss">
	@use "sass:color";

	@function hex($color) {
		@return "#" + str-slice(color.ie-hex-str($color), 4);
	}

	canvas {
		width: 100%;
		height: 100%;

		--gradient-color-1: #{hex(oklch(30% 0.15 -15))};
		--gradient-color-2: #{hex(oklch(40% 0.15 15))};
		--gradient-color-3: #{hex(oklch(50% 0.15 45))};
		--gradient-color-4: #{hex(oklch(50% 0.15 75))};

		background-color: var(--gradient-color-1);
	}
</style>
