import { Component, input } from '@angular/core';
import { Korisnik } from '../../core';

@Component({
  selector: 'app-participants',
  standalone: true,
  imports: [],
  templateUrl: './participants.component.html',
  styleUrl: './participants.component.scss',
})
export class ParticipantsComponent {
  public participants = input.required<Korisnik[]>();
}
