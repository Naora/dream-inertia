<script>
	import { Link, Deferred } from "@inertiajs/svelte";
	
	let { permissions, user } = $props()
</script>

<h1>Hello Home {user}!</h1>
<Link href="/about">About page</Link>
<Deferred data="permissions">
  {#snippet fallback()}
	Loading...
  {/snippet}

	{permissions.kind} 
</Deferred>
