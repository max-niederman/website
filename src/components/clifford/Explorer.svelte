<script lang="ts">
	import Attractor from "./Attractor.svelte";

	let dynamics = {
		a: -1.4,
		b: 1.6,
		c: 1.0,
		d: 0.7,
	};
	let numPoints = 1_000_000;
	let attractorKey = {};

	$: attractorKey = numPoints;
</script>

<form action="none">
	<math display="inline">
		<mi>a</mi>
		<mo>=</mo>
		<mn>{dynamics.a}</mn>
	</math>
	<input type="range" bind:value={dynamics.a} min={-3} max={3} step={0.01} />
	<math display="inline">
		<mi>b</mi>
		<mo>=</mo>
		<mn>{dynamics.b}</mn>
	</math>
	<input type="range" bind:value={dynamics.b} min={-3} max={3} step={0.01} />
	<math display="inline">
		<mi>c</mi>
		<mo>=</mo>
		<mn>{dynamics.c}</mn>
	</math>
	<input type="range" bind:value={dynamics.c} min={-3} max={3} step={0.01} />
	<math display="inline">
		<mi>d</mi>
		<mo>=</mo>
		<mn>{dynamics.d}</mn>
	</math>
	<input type="range" bind:value={dynamics.d} min={-3} max={3} step={0.01} />

	Number of points: {numPoints.toLocaleString()}
	<input
		type="range"
		bind:value={numPoints}
		min={0}
		max={10_000_000}
		step={100_000}
	/>
	Reset the attractor if it has become too noisy.
	<button type="button" on:click={() => (attractorKey = {})}>Reset</button>
</form>

<div class="display-container">
	{#key attractorKey}
		<Attractor
			dynamics={() => dynamics}
			display={() => ({ scale: [0.25, 0.25], offset: [0, 0] })}
			{numPoints}
		/>
	{/key}
</div>

<style lang="scss">
	.display-container {
		aspect-ratio: 1.618;
	}

	input,
	button {
		display: block;
	}
</style>
