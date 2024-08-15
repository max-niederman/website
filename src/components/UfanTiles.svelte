<!-- Inspired by Lee Ufan's _From Point_ series of paintings. -->

<script lang="ts">
	import { onMount } from "svelte";
	import { resizeCanvasToDisplaySize } from "~/utils/canvas";
	import { Vec2 } from "~/utils/math";

	export let tileSize: number = 100;
	export let gap: number = 10;

	let canvas: HTMLCanvasElement;

	onMount(() => {
		resizeCanvasToDisplaySize(canvas);

		let ctx = canvas.getContext("2d");
		if (!ctx) return;

		const draw = (now: DOMHighResTimeStamp) => {
			resizeCanvasToDisplaySize(canvas);
			const layout = computeLayout();

			for (let i = 0; i < layout.count.x; i++) {
				for (let j = 0; j < layout.count.y; j++) {
					ctx.fillStyle = `hsl(${(i + j) * 10 + now / 10}, 100%, 50%)`;
					ctx.beginPath();
					ctx.roundRect(
						layout.start.x + i * (tileSize + gap),
						layout.start.y + j * (tileSize + gap),
						tileSize,
						tileSize,
						20,
					);
					ctx.fill();
				}
			}

			requestAnimationFrame(draw);
		};

		requestAnimationFrame(draw);
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
