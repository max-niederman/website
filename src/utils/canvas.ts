/** Resize the canvas to match the size at which it is displayed. */
export function resizeCanvasToDisplaySize(canvas: HTMLCanvasElement): boolean {
	const width = canvas.clientWidth;
	const height = canvas.clientHeight;

	const needResize = canvas.width !== width || canvas.height !== height;

	if (needResize) {
		canvas.width = width;
		canvas.height = height;
	}

	return needResize;
}
