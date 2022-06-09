# minek8s

a minecraft server kubernetes operator.

## what

this is a kubernetes operator that allows for easy provisioning of minecraft servers, written in haskell.
it lives in a kubernetes cluster and watches for `MinecraftInstance` crd instances.

upon creation / update of a `MinecraftInstance` crd instance in a namespace, it will create / update the following in that namespace:
* kubernetes `StatefulSet` with 1 replica for the minecraft server itself
* kubernetes `Service` to front the minecraft server

upon deletion of a `MinecraftInstance`, the operator will clean up the above.

## todo

* implement everything described in the previous section
* document the schema of the `MinecraftInstance` crd
* deployment method for everything described in the previous section, including documentation for deployment
* minecraft server proxy to allow for spin up / shutdown of minecraft server pods via `StatefulSet` based on number of players connected
