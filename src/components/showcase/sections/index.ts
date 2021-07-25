import Name from "./Name.svelte";
import Work from "./Work.svelte";

export interface Section {
  id: string;
  title?: string;
  shortTitle?: string;
  content: number;
}

const sections: Section[] = [
  {
    id: "name",
    content: Name,
  },
  {
    id: "work",
    title: "My Work",
    shortTitle: "Work",
    content: Work,
  }
];

export default sections;
