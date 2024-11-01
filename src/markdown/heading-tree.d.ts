export type HeadingTree = {
	data: Heading;
	children: HeadingTree[];
};

export type Heading = {
	id: string;
	depth: number;
	content: string;
};
