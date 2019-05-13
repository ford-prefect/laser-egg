# Kaiterra Laser Egg AQI data as Prometheus metrics

This is a simple project to read air quality data from the Kaiterra Laser Egg 2
and expose that as a Prometheus client. The data is also written out to a file.

To use:

  * Get [stack](https://haskellstack.org/)
  * Do a `stack build`
  * Export `KAITERRA_API_KEY` and `KAITERRA_DEVICE_UUID` in your environment
  * Do a `stack run`

The API is documented at https://dev.kaiterra.com/.

This code will currently only work with the Laser Egg 2, but can easily be
extended for other devices.

## Deploying

A Kubernetes deployment to visualise this data via Prometheus and Grafana is in
`deploy/aqi.yml`. To use this, you can do the following:

```sh
# Set up your KUBECONFIG as appropriate
$ kubectl create secret generic kaiterra --from-literal=api-key=<API_KEY> --from-literal=device-uuid=<UUID>
$ kubectl apply -f deploy/aqi.yml
```
