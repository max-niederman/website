import { z, defineCollection } from "astro:content";

const postsCollection = defineCollection({
	type: "content",
	schema: z.object({
		title: z.string(),
		published: z.date(),
		updated: z.date(),
		crossposts: z.object({
			lesswrong: z.string().optional(),
		}),
	}),
});

const tradesCollection = defineCollection({
	type: "content",
	schema: z.object({
		summary: z.string(),
		instruments: z
			.object({
				namespace: z.enum(["New York Stock Exchange", "Polymarket"]),
				ticker: z.string().optional(),
				name: z.string(),
			})
			.array(),
		entry: z.date(),
		exit: z.date(),
		spent: z.number(),
		received: z.number(),
	}),
});

export const collections = {
	posts: postsCollection,
	trades: tradesCollection,
};
