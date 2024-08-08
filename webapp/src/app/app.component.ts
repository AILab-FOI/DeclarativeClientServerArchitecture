import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import init, { dohvati_studente } from '../assets/pkg/client';
import { Student } from '../assets/pkg/client';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
})
export class AppComponent {
  title = 'client';
  studenti: Promise<Student[]>;

  constructor() {
    this.loadWasm();
  }

  async loadWasm() {
    init().then((wasm) => {
      dohvati_studente().then((studenti) => {
        console.log(studenti);
      });
    });
  }

  async dohvati(): Promise<Student[]> {
    return this.studenti;
  }
}
