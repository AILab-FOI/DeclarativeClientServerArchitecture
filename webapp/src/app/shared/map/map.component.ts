import { Component, effect, input } from '@angular/core';
import * as L from 'leaflet';

type LatLng = {
  lat: number;
  lng: number;
};

@Component({
  selector: 'app-map',
  standalone: true,
  imports: [],
  templateUrl: './map.component.html',
  styleUrl: './map.component.scss',
})
export class MapComponent {
  public latLng = input.required<LatLng>();
  constructor() {
    effect(() => {
      this.initMap(this.latLng());
    });
  }
  private initMap(latLong: { lat: number; lng: number }): void {
    let map = L.map('map', {
      center: latLong,
      zoom: 6,
    });
    getLayers().forEach((tile) => {
      tile.addTo(map);
    });
    map.scrollWheelZoom.disable();
    map.dragging.disable();
    map.zoomControl.remove();
    map.addLayer(new L.Marker(latLong));
  }
}
export const getLayers = (): L.Layer[] => {
  return [
    new L.TileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; OpenStreetMap contributors',
    } as L.TileLayerOptions),
  ] as L.Layer[];
};
