import { z, defineCollection } from "astro:content";

const postsCollection = defineCollection({
	type: "content",
	schema: z.object({
		title: z.string(),
		published: z.date(),
		updated: z.date(),
	}),
});

export const collections = {
	posts: postsCollection,
};
