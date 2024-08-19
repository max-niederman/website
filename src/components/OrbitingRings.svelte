<script lang="ts">
	import Alea from "alea";
	import { onDestroy, onMount } from "svelte";
	import { resizeCanvasToPhysicalDisplaySize } from "~/utils/canvas";
	import { TAU, Vec2, randInterval } from "~/utils/math";

	export let minRadius: number;
	export let arcWidth: number;
	export let arcSpacing: number;

	let canvas: HTMLCanvasElement;

	onMount(() => {
		const ctx = canvas.getContext("2d");
		if (!ctx) return;

		const seed = Math.random();
		const minRadiusPhy = minRadius * window.devicePixelRatio;
		const arcWidthPhy = arcWidth * window.devicePixelRatio;
		const arcSpacingPhy = arcSpacing * window.devicePixelRatio;

		function draw(now: DOMHighResTimeStamp) {
			if (!ctx) return;

			resizeCanvasToPhysicalDisplaySize(canvas);
			ctx.clearRect(0, 0, canvas.width, canvas.height);

			const prng = Alea(seed);

			const center = new Vec2(canvas.width, canvas.height).div(2);

			ctx.lineWidth = arcWidthPhy;
			ctx.lineCap = "round";

			for (let i = 0; i < 100; i++) {
				const radius = minRadiusPhy + i * arcSpacingPhy;
				if (radius >= center.l2Norm()) break;

				const startAngle = prng() * TAU;
				const angularVelocity = randInterval(0.1, 0.2, prng) * TAU;
				const angle = randInterval(0.3, 0.7, prng) * TAU;

				ctx.beginPath();
				ctx.arc(
					center.x,
					center.y,
					radius,
					startAngle + (now / 1000) * angularVelocity,
					startAngle + (now / 1000) * angularVelocity + angle,
				);
				ctx.stroke();
			}

			requestAnimationFrame(draw);
		}

		requestAnimationFrame(draw);
	});
</script>

<canvas bind:this={canvas} />

<style lang="scss">
	canvas {
		width: 100%;
		height: 100%;
	}
</style>
