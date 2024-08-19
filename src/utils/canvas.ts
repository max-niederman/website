/** Resize the canvas to match the size at which it is displayed, in physical pixels. */
export function resizeCanvasToPhysicalDisplaySize(
	canvas: HTMLCanvasElement
): boolean {
	const width = canvas.clientWidth * window.devicePixelRatio;
	const height = canvas.clientHeight * window.devicePixelRatio;

	const needResize = canvas.width !== width || canvas.height !== height;

	if (needResize) {
		canvas.width = width;
		canvas.height = height;
	}

	return needResize;
}
