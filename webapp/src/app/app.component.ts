import { Component, signal } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import init, { dohvati_studente, InitOutput } from '../assets/pkg/client';
import { Student } from '../assets/pkg/client';
import { NavigationComponent } from './shared';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, NavigationComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
})
export class AppComponent {
  title = 'client';
  init: Promise<InitOutput>;
  studenti = signal<Student[]>([]);
  constructor() {
    this.init = init();
    this.loadWasm();
  }

  async loadWasm() {
    // this.init.then(() => {
    //   dohvati_studente('').then((studenti: { data: Student[] }) => {
    //     this.studenti.update((s) => studenti.data);
    //   });
    // });
  }
}
