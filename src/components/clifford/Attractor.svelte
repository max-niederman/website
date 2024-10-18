<script lang="ts">
	import { onDestroy, onMount } from "svelte";
	import { Vec2 } from "~/utils/math";
	import { resizeCanvasToPhysicalDisplaySize } from "~/utils/canvas";

	export let dynamics: (now: DOMHighResTimeStamp) => {
		a: number;
		b: number;
		c: number;
		d: number;
	};
	export let display: (now: DOMHighResTimeStamp) => {
		scale?: Vec2;
		offset?: Vec2;
	};
	export let numPoints: number = 1_000_000;

	let canvas: HTMLCanvasElement;

	let webglSupported = true;

	onMount(() => {
		const gl = canvas.getContext("webgl2", { antialias: true });

		if (!gl) {
			console.error("WebGL2 is not supported.");

			webglSupported = false;

			return;
		}

		const updater = new Updater(gl);
		const renderer = new Renderer(gl);

		updater.overwritePositions(
			new Float32Array(2 * numPoints).map(() => Math.random() * 2 - 1),
		);

		gl.enable(gl.BLEND);
		gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

		function draw(now: DOMHighResTimeStamp) {
			resizeCanvasToPhysicalDisplaySize(canvas);

			gl.viewport(0, 0, canvas.width, canvas.height);

			const { a, b, c, d } = dynamics(now);
			const { scale = new Vec2(1), offset = new Vec2() } = display(now);

			updater.update(a, b, c, d);
			renderer.draw(
				updater.readBuffer,
				new Vec2(canvas.height / canvas.width, 1).hadamardProd(scale),
				offset,
			);

			requestAnimationFrame(draw);
		}

		requestAnimationFrame(draw);
	});

	class Updater {
		#gl: WebGL2RenderingContext;

		#program: WebGLProgram;
		#uniformLocations: Record<
			"a" | "b" | "c" | "d" | "cursor",
			WebGLUniformLocation
		>;
		#attribLocations: Record<"in_position", number>;

		// Buffers used to store the particle positions.
		#buffers: WebGLBuffer[];

		// The index of the buffer which should next be read from.
		#readBufferIndex = 0;

		constructor(gl: WebGL2RenderingContext) {
			this.#gl = gl;

			const vertexShader = createShader(
				gl,
				gl.VERTEX_SHADER,
				`#version 300 es

				precision highp float;

				uniform float a;
				uniform float b;
				uniform float c;
				uniform float d;

				in vec2 in_position;

				out vec2 out_position;

				vec2 attract(vec2 pos) {
					return sin(vec2(a, b) * pos.yx)
							+ vec2(c, d) * cos(vec2(a, b) * pos);
				}

				void main() {
					out_position = attract(in_position);
				}`,
			);

			const fragmentShader = createShader(
				gl,
				gl.FRAGMENT_SHADER,
				`#version 300 es
        
				precision lowp float;

				out vec4 fragColor;
				
				void main() {
					fragColor = vec4(0);
				}`,
			);

			if (!vertexShader || !fragmentShader) {
				throw new Error("Failed to create shaders.");
			}
			const program = createProgram(gl, vertexShader, fragmentShader, [
				"out_position",
			]);

			if (!program) {
				throw new Error("Failed to create program.");
			}

			this.#program = program;
			this.#uniformLocations = {
				a: gl.getUniformLocation(program, "a"),
				b: gl.getUniformLocation(program, "b"),
				c: gl.getUniformLocation(program, "c"),
				d: gl.getUniformLocation(program, "d"),
			};
			this.#attribLocations = {
				in_position: gl.getAttribLocation(program, "in_position"),
			};

			this.#buffers = [
				createBuffer(gl, 2 * 4 * numPoints, gl.STREAM_DRAW),
				createBuffer(gl, 2 * 4 * numPoints, gl.STREAM_DRAW),
			];
		}

		overwritePositions(positions: Float32Array) {
			const gl = this.#gl;

			gl.bindBuffer(gl.ARRAY_BUFFER, this.#buffers[0]);
			gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STREAM_DRAW);
		}

		update(a: number, b: number, c: number, d: number) {
			const gl = this.#gl;

			gl.useProgram(this.#program);

			// Set all the appropriate uniforms.
			gl.uniform1f(this.#uniformLocations.a, a);
			gl.uniform1f(this.#uniformLocations.b, b);
			gl.uniform1f(this.#uniformLocations.c, c);
			gl.uniform1f(this.#uniformLocations.d, d);

			// Bind the read buffer.
			gl.bindBuffer(gl.ARRAY_BUFFER, this.readBuffer);
			gl.enableVertexAttribArray(this.#attribLocations.in_position);
			gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);

			// Bind the write buffer as transform feedback.
			gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, this.writeBuffer);

			// We're not actually drawing anything, so we can discard the rasterizer output.
			gl.enable(gl.RASTERIZER_DISCARD);

			// Perform the transform feedback operation.
			gl.beginTransformFeedback(gl.POINTS);
			gl.drawArrays(gl.POINTS, 0, numPoints);
			gl.endTransformFeedback();

			// Unbind the write buffer.
			gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, null);

			// Re-enable the rasterizer.
			gl.disable(gl.RASTERIZER_DISCARD);

			// Finally, swap the read and write buffers.
			this.#readBufferIndex = 1 - this.#readBufferIndex;
		}

		get readBuffer(): WebGLBuffer {
			return this.#buffers[this.#readBufferIndex];
		}

		get writeBuffer(): WebGLBuffer {
			return this.#buffers[1 - this.#readBufferIndex];
		}
	}

	class Renderer {
		#gl: WebGL2RenderingContext;

		#program: WebGLProgram;
		#uniformLocations: Record<"scale" | "offset", WebGLUniformLocation>;
		#attribLocations: Record<"position", number>;

		constructor(gl: WebGL2RenderingContext) {
			this.#gl = gl;

			const vertexShader = createShader(
				gl,
				gl.VERTEX_SHADER,
				`#version 300 es

				uniform vec2 scale;
				uniform vec2 offset;

				in vec2 position;

				void main() {
					gl_Position = vec4(position * scale + offset, 0.0, 1.0);
					gl_PointSize = 1.0;
				}`,
			);

			const fragmentShader = createShader(
				gl,
				gl.FRAGMENT_SHADER,
				`#version 300 es

				precision lowp float;

				out vec4 fragColor;

				void main() {
					fragColor = vec4(0.5, 0.5, 1.0, 0.02);
				}`,
			);

			if (!vertexShader || !fragmentShader) {
				throw new Error("Failed to create shaders.");
			}
			const program = createProgram(gl, vertexShader, fragmentShader);

			if (!program) {
				throw new Error("Failed to create program.");
			}

			this.#program = program;
			this.#uniformLocations = {
				scale: gl.getUniformLocation(program, "scale"),
				offset: gl.getUniformLocation(program, "offset"),
			};
			this.#attribLocations = {
				position: gl.getAttribLocation(program, "position"),
			};
		}

		draw(positions: WebGLBuffer, scale: Vec2, offset: Vec2) {
			const gl = this.#gl;

			gl.useProgram(this.#program);

			// Set the uniforms.
			gl.uniform2f(this.#uniformLocations.scale, scale.x, scale.y);
			gl.uniform2f(this.#uniformLocations.offset, offset.x, offset.y);

			// Bind the positions buffer.
			gl.bindBuffer(gl.ARRAY_BUFFER, positions);
			gl.enableVertexAttribArray(this.#attribLocations.position);
			gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);

			// Clear the canvas before drawing.
			gl.clearColor(0.0, 0.0, 0.0, 1.0);
			gl.clear(gl.COLOR_BUFFER_BIT);

			// Finally, draw the points.
			gl.drawArrays(gl.POINTS, 0, numPoints);
		}
	}

	/** Create a WebGL buffer with the given data. */
	function createBuffer(
		gl: WebGL2RenderingContext,
		data: BufferSource | GLsizeiptr,
		usage: GLenum = gl.STATIC_DRAW,
	): WebGLBuffer {
		const buffer = gl.createBuffer() as WebGLBuffer;
		gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
		gl.bufferData(gl.ARRAY_BUFFER, data, usage);
		return buffer;
	}

	/** Create a WebGL shader. */
	function createShader(
		gl: WebGL2RenderingContext,
		type: number,
		source: string,
	): WebGLShader | null {
		const shader = gl.createShader(type) as WebGLShader;
		gl.shaderSource(shader, source);
		gl.compileShader(shader);

		if (gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
			return shader;
		}

		console.error(gl.getShaderInfoLog(shader));
		gl.deleteShader(shader);
		return null;
	}

	/** Create a WebGL shader program. */
	function createProgram(
		gl: WebGL2RenderingContext,
		vertexShader: WebGLShader,
		fragmentShader: WebGLShader,
		transformFeedbackVaryings: string[] = [],
	): WebGLProgram | null {
		const program = gl.createProgram() as WebGLProgram;
		gl.attachShader(program, vertexShader);
		gl.attachShader(program, fragmentShader);

		if (transformFeedbackVaryings) {
			gl.transformFeedbackVaryings(
				program,
				transformFeedbackVaryings,
				gl.SEPARATE_ATTRIBS,
			);
		}

		gl.linkProgram(program);

		if (gl.getProgramParameter(program, gl.LINK_STATUS)) {
			return program;
		}

		console.error(gl.getProgramInfoLog(program));
		gl.deleteProgram(program);
		return null;
	}
</script>

{#if webglSupported}
	<canvas bind:this={canvas} />
{:else}
	<div class="no-webgl2">Your browser does not support WebGL2.</div>
{/if}

<style lang="scss">
	canvas,
	.no-webgl2 {
		width: 100%;
		height: 100%;
	}

	.no-webgl2 {
		display: flex;
		justify-content: center;
		align-items: center;
		font-size: 2em;

		margin-top: 16px 0 0 0;

		background-color: black;
		color: white;
	}
</style>
