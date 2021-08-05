import Name from "./Name.svelte";
import Work from "./Work.svelte";
import Writings from "./Writings.svelte";

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
  },
  {
    id: "writings",
    title: "My Writings",
    shortTitle: "Writings",
    content: Writings,
  },
];

export default sections;
