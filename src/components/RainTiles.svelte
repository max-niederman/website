<!-- Inspired by Lee Ufan's _From Point_ series of paintings. -->

<script lang="ts">
	import { onMount } from "svelte";
	import { resizeCanvasToDisplaySize } from "~/utils/canvas";
	import { Vec2 } from "~/utils/math";

	export let tileSize: number = 100;
	export let gap: number = 0;

	export let dropInterval: number = 0.4;
	export let tickInterval: number = 0.25;

	let canvas: HTMLCanvasElement;

	const trailLength = 4;
	const colorPalette = `
		#ad00ff
		#8c42ff
		#685bff
		#416bff
		#0077ff
	`
		.split("\n")
		.map((s) => s.trim());

	onMount(() => {
		resizeCanvasToDisplaySize(canvas);

		let ctx = canvas.getContext("2d");
		if (!ctx) return;

		const layout = computeLayout();

		let lastDrop = 0;
		let dropTimes = Array(layout.count.x).fill(null);

		const draw = (now: DOMHighResTimeStamp) => {
			resizeCanvasToDisplaySize(canvas);
			const layout = computeLayout();

			if (layout.count.x > dropTimes.length) {
				dropTimes.push(...Array(layout.count.x - dropTimes.length).fill(null));
			} else if (layout.count.x < dropTimes.length) {
				dropTimes = dropTimes.slice(0, layout.count.x);
			}

			if (now - lastDrop >= 1000 * dropInterval) {
				let col = Math.floor(Math.random() * layout.count.x);
				dropTimes[col] = now;
				lastDrop = now;
			}

			for (let col = 0; col < layout.count.x; col++) {
				if (dropTimes[col] === null) continue;

				// clear the column
				ctx.fillStyle = "black";
				ctx.fillRect(
					layout.start.x + col * (tileSize + gap),
					layout.start.y,
					tileSize,
					layout.count.y * (tileSize + gap),
				);

				const dropRow = Math.floor(
					(now - dropTimes[col]) / (1000 * tickInterval),
				);

				if (dropRow - trailLength >= layout.count.y) {
					dropTimes[col] = null;
					continue;
				}

				// draw upward trail
				for (
					let row = Math.min(dropRow, layout.count.y - 1);
					row >= Math.max(dropRow - trailLength, 0);
					row--
				) {
					ctx.beginPath();
					ctx.fillStyle = colorPalette[dropRow - row];
					ctx.roundRect(
						layout.start.x + col * (tileSize + gap),
						layout.start.y + row * (tileSize + gap),
						tileSize,
						tileSize,
						0,
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

		background-color: black;
	}
</style>
