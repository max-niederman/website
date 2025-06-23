<script lang="ts">
	import type { HeadingTree } from "~/markdown/heading-tree";

	export let headingTree: HeadingTree | undefined = undefined;
</script>

<div class="container">
	<nav>
		<div class="top-row">
			<a class="home basic-link" href="/">MN</a>
			<div class="spacer" />
			<a class="basic-link" href="/#writing">Writing</a>
		</div>
	</nav>
</div>

<style lang="scss">
	@use "~/styles/links.scss";

	nav {
		padding: 0.5rem 1rem;

		border-bottom: 1px solid var(--col-fg2);

		text-align: center;
	}

	.home {
		font-size: 1.35rem;
		font-style: italic;
		
		font-feature-settings: "calt";
		&::first-letter {
			font-feature-settings: "calt", "swsh";
		}
	}

	.top-row {
		display: flex;
		align-items: center;
	}

	.spacer {
		flex-grow: 1;
	}

	@media (min-width: 1720px) {
		.container {
			position: fixed;
			left: 0;
			top: 0;
			width: calc((100vw - 80ch) / 2);
		}

		nav {
			width: 40ch;
            margin: 0 auto;
		}
	}
</style>
