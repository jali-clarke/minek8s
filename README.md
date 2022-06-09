# minek8s

a minecraft server kubernetes operator.

## what

this is a kubernetes operator that allows for easy provisioning of minecraft servers, written in haskell.
it lives in a kubernetes cluster and watches for `MinecraftInstance` crd instances.

upon creation / update of a `MinecraftInstance` crd instance in a namespace, it will create / update the following in that namespace:
* [kubernetes `StatefulSet`](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/) with 1 replica for the minecraft server itself
  * the container image used comes from https://github.com/itzg/docker-minecraft-server
* [kubernetes `Service`](https://kubernetes.io/docs/concepts/services-networking/service/) (of [type `NodePort`](https://kubernetes.io/docs/concepts/services-networking/service/#type-nodeport) for now) to front the minecraft server

upon deletion of a `MinecraftInstance`, the operator will clean up the above.

the definition of the crd can be found at [./manifests/crd.yaml](./manifests/crd.yaml).
here's an example of one you'd create yourself:

```yaml
apiVersion: jali-clarke.ca/v1
kind: MinecraftInstance
metadata:
  name: my-minecraft-server
  namespace: some-namespace
spec:
  minecraftVersion: 1.2.5
  nodePortService:
    name: my-minecraft-service
    nodePort: 25565
```

## todo

* implement everything described in the previous section
* deployment method for everything described in the previous section, including documentation for deployment
* minecraft server proxy to allow for spin up / shutdown of minecraft server pods via `StatefulSet` based on number of players connected
