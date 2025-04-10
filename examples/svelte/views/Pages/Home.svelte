<script>
	import { Link, Deferred } from "@inertiajs/svelte";
	
	let { permissions } = $props()
</script>

<h1>Hello Home page!</h1>
<Link href="/about">About page</Link>
<Deferred data="permissions">
  {#snippet fallback()}
	Loading...
  {/snippet}

	{permissions.kind} 
</Deferred>
