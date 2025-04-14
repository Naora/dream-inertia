<script>
	import { Link, Deferred,  router } from "@inertiajs/svelte";
	
	let { permissions, user, events, event } = $props()
</script>

<h1>Hello Home {user}!</h1>
<Link href="/about">About page</Link>

<p>you currenlty have this event actif "{event.title}"</p>

<Deferred data="permissions">
  {#snippet fallback()}
	Loading...
  {/snippet}

	{permissions.kind} 
</Deferred>


<ul>
    {#each events as otherEvent}
      <li>{otherEvent.title}</li>
    {/each}
</ul>
<button onclick={router.visit("/", {only: ['events']})} >Other events</button>
