<script lang="ts">
	import Attractor from "./Attractor.svelte";

	let dynamics = {
		a: -1.4,
		b: 1.6,
		c: 1.0,
		d: 0.7,
	};
</script>

<form action="none">
	<input type="range" bind:value={dynamics.a} min={-2} max={2} step={0.01} />
</form>

<div class="display-container">
	<Attractor
		dynamics={() => dynamics}
		display={() => ({ scale: [0.5, 0.5], offset: [0, 0] })}
	/>
</div>

<style lang="scss">
	.display-container {
		aspect-ratio: 1.618;
	}
</style>
