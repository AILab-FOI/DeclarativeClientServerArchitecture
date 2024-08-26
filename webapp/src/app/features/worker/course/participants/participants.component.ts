import { Component, input } from '@angular/core';
import { Korisnik } from '../../../../core';

@Component({
  selector: 'app-worker-participants',
  standalone: true,
  imports: [],
  templateUrl: './participants.component.html',
  styleUrl: './participants.component.scss',
})
export class WorkerParticipantsComponent {
  public participants = input.required<Korisnik[]>();
}
