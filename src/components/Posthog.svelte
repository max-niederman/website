<script lang="ts">
	import posthog from "posthog-js";
	import { onMount } from "svelte";

	onMount(() => {
		posthog.init("phc_t6xmW3mX5ce4NWuZ5vJCjkugMh4voIWD2aUvNgNbEZp", {
			api_host: "https://us.i.posthog.com",
			person_profiles: "identified_only",
		});
	});
</script>
