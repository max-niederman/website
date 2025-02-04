export class Vec2 {
	readonly #x: number;
	readonly #y: number;

	constructor(x: number = 0, y: number = x) {
		this.#x = x;
		this.#y = y;
	}

	get x(): number {
		return this.#x;
	}

	get y(): number {
		return this.#y;
	}

	add(other: Vec2): Vec2 {
		return new Vec2(this.#x + other.#x, this.#y + other.#y);
	}

	sub(other: Vec2): Vec2 {
		return new Vec2(this.#x - other.#x, this.#y - other.#y);
	}

	mul(scalar: number): Vec2 {
		return new Vec2(this.#x * scalar, this.#y * scalar);
	}

	div(scalar: number): Vec2 {
		return new Vec2(this.#x / scalar, this.#y / scalar);
	}

	dot(other: Vec2): number {
		return this.#x * other.#x + this.#y * other.#y;
	}

	hadamardProd(other: Vec2): Vec2 {
		return new Vec2(this.#x * other.#x, this.#y * other.#y);
	}

	hadamardDiv(other: Vec2): Vec2 {
		return new Vec2(this.#x / other.#x, this.#y / other.#y);
	}

	componentwise(func: (x: number) => number): Vec2 {
		return new Vec2(func(this.#x), func(this.#y));
	}

	l1Norm(): number {
		return Math.abs(this.#x) + Math.abs(this.#y);
	}

	l2Norm(): number {
		return Math.sqrt(this.#x ** 2 + this.#y ** 2);
	}

	lInfNorm(): number {
		return Math.max(Math.abs(this.#x), Math.abs(this.#y));
	}
}

export function randInterval(
	min: number,
	max: number,
	rng: () => number = Math.random,
): number {
	return min + rng() * (max - min);
}

export const TAU = 2 * Math.PI;
