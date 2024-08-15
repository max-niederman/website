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

    hademardProd(other: Vec2): Vec2 {
        return new Vec2(this.#x * other.#x, this.#y * other.#y);
    }

    hademardDiv(other: Vec2): Vec2 {
        return new Vec2(this.#x / other.#x, this.#y / other.#y);
    }

    componentwise(func: (x: number) => number): Vec2 {
        return new Vec2(func(this.#x), func(this.#y));
    }
}