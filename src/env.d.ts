/// <reference path="../.astro/types.d.ts" />
/// <reference types="astro/client" />

interface ImportMetaEnv {
    readonly PUBLIC_POSTHOG_TOKEN?: string;
    readonly PUBLIC_POSTHOG_API_HOST?: string;
    readonly PUBLIC_POSTHOG_PERSON_PROFILES?: "always" | "never" | "identified_only";
}

interface ImportMeta {
    readonly env: ImportMetaEnv;
}
