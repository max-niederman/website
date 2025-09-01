<script lang="ts">
	import posthog from "posthog-js";
	import { onMount } from "svelte";

	// Read configuration from PUBLIC_ env variables so we don't have to pass props around
	const envToken = import.meta.env.PUBLIC_POSTHOG_TOKEN;
	const envApiHost = import.meta.env.PUBLIC_POSTHOG_API_HOST;
	const envPersonProfiles = import.meta.env.PUBLIC_POSTHOG_PERSON_PROFILES as
		| "always"
		| "never"
		| "identified_only"
		| undefined;

	const resolvedApiHost: string = envApiHost ?? "https://us.i.posthog.com";
	const resolvedPersonProfiles: "always" | "never" | "identified_only" =
		envPersonProfiles === "always" || envPersonProfiles === "never" || envPersonProfiles === "identified_only"
			? envPersonProfiles
			: "identified_only";

	onMount(() => {
		if (!envToken) {
			console.warn("PostHog token is not set. Set it in your env to enable analytics.");
			return;
		}

		posthog.init(envToken, {
			api_host: resolvedApiHost,
			person_profiles: resolvedPersonProfiles,
		});
	});
</script>
