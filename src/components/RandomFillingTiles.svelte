<!-- Inspired by Lee Ufan's _From Point_ series of paintings. -->

<script lang="ts">
	import { onDestroy, onMount } from "svelte";
	import { resizeCanvasToDisplaySize } from "~/utils/canvas";
	import { Vec2 } from "~/utils/math";

	export let tileSize: number = 100;
	export let gap: number = 10;

	export let tickInterval: number = 0.4;

	let canvas: HTMLCanvasElement;

	const colorPalette = `
		#ad00ff
		#8c42ff
		#685bff
		#416bff
		#0077ff
	`
		.split("\n")
		.map((s) => s.trim())
		.filter(Boolean);

	let tickerInterval: number;
	let resizeCb: () => void;

	onMount(() => {
		let ctx = canvas.getContext("2d");
		if (!ctx) return;

		resizeCanvasToDisplaySize(canvas);

		let layout = computeLayout();

		resizeCb = () => {
			if (resizeCanvasToDisplaySize(canvas)) {
				ctx?.clearRect(0, 0, canvas.width, canvas.height);
				layout = computeLayout();
			}
		};
		window.addEventListener("resize", resizeCb);

		tickerInterval = setInterval(() => {
			let pos = new Vec2(Math.random(), Math.random())
				.hademardProd(layout.count)
				.componentwise(Math.floor);
			let color = colorPalette[Math.floor(Math.random() * colorPalette.length)];

			ctx.fillStyle = color;
			ctx?.beginPath();
			ctx?.roundRect(
				layout.start.x + pos.x * (tileSize + gap),
				layout.start.y + pos.y * (tileSize + gap),
				tileSize,
				tileSize,
				10,
			);
			ctx?.fill();
		}, tickInterval * 1000);
	});

	onDestroy(() => {
		clearInterval(tickerInterval);
		window.removeEventListener("resize", resizeCb);
	});

	type Layout = {
		start: Vec2;
		count: Vec2;
	};

	function computeLayout(): Layout {
		const size = new Vec2(canvas.width, canvas.height);
		const available = size.sub(new Vec2(2 * gap));
		const count = available
			.hademardDiv(new Vec2(tileSize + gap))
			.componentwise(Math.floor);
		const start = size.sub(count.mul(tileSize + gap)).div(2);

		return { start, count };
	}
</script>

<canvas bind:this={canvas} />

<style lang="scss">
	canvas {
		width: 100%;
		height: 100%;
	}
</style>
