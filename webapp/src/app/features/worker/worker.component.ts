import { Component } from '@angular/core';
import { RouterModule } from '@angular/router';

@Component({
  selector: 'app-worker',
  standalone: true,
  imports: [RouterModule],
  templateUrl: './worker.component.html',
  styleUrl: './worker.component.scss',
})
export class WorkerComponent {}
